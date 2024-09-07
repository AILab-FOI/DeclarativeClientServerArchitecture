import {
  Component,
  computed,
  inject,
  model,
  output,
  signal,
  TemplateRef,
  viewChild,
} from '@angular/core';
import { InputComponent } from '../input/input.component';
import { Sadrzaj, TokenService } from '../../core';
import { CommonModule } from '@angular/common';
import { upload_file } from '../../../assets/pkg/client';

@Component({
  selector: 'app-content-add',
  standalone: true,
  imports: [InputComponent, CommonModule],
  templateUrl: './content-add.component.html',
  styleUrl: './content-add.component.scss',
})
export class ContentAddComponent {
  data = model.required<Sadrzaj>();

  query = output<Sadrzaj>();
  token = inject(TokenService);

  selected = signal<string>('dokument');

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  editValue(field: string, value: unknown): void {
    this.data.update((d) => {
      d.vrijednost[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  editType(value: any): void {
    this.data.update((d) => {
      d.tip = value;
      return d;
    });
    this.selected.set(value);
    this.query.emit(this.data());
  }

  fileUpload(event: Event) {
    const file = (event.target as HTMLInputElement).files?.[0];

    if (file) {
      const reader = new FileReader();
      reader.onload = () => {};
      reader.readAsDataURL(file);
      upload_file(file, this.token.accessToken()).then((res) => {
        this.data.update((d) => {
          d.vrijednost.referenca = res;
          return d;
        });
      });
    }
  }
}
