import { Component, inject, model, output, signal } from '@angular/core';
import { defaultKorisnik, Korisnik, TokenService } from '../../core';
import { InputComponent } from '../input/input.component';
import { upload_file } from '../../../assets/pkg/client';
import { NgOptimizedImage } from '@angular/common';

@Component({
  selector: 'app-user-edit',
  standalone: true,
  imports: [InputComponent, NgOptimizedImage],
  templateUrl: './user-edit.component.html',
  styleUrl: './user-edit.component.scss',
})
export class UserEditComponent {
  data = model<Korisnik>(defaultKorisnik());
  query = output<Korisnik>();
  imgSource = signal<any>(undefined);
  token = inject(TokenService);

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }
  editDodatno(field: string, value: unknown): void {
    this.data.update((d) => {
      d.dodatno[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  fileUpload(event: Event) {
    const file = (event.target as HTMLInputElement).files?.[0];

    if (file) {
      const reader = new FileReader();
      reader.onload = () => {
        this.imgSource.set(reader.result);
      };
      reader.readAsDataURL(file);
      upload_file(file, this.token.accessToken()).then((res) => {
        this.data.update((d) => {
          d.slika = res;
          return d;
        });
      });
    }
  }
}
