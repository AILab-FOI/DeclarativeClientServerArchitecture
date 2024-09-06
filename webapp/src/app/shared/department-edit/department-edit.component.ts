import { Component, model, output, signal } from '@angular/core';
import { Katedra } from '../../core';
import { upload_file } from '../../../assets/pkg/client';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-department-edit',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './department-edit.component.html',
  styleUrl: './department-edit.component.scss',
})
export class DepartmentEditComponent {
  data = model<Katedra>(undefined);
  query = output<Katedra>();
  imgSource = signal<any>(undefined);

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  async fileUpload(event: Event) {
    const file = (event.target as HTMLInputElement).files?.[0];

    if (file) {
      const reader = new FileReader();
      reader.onload = () => {
        this.imgSource.set(reader.result);
      };
      reader.readAsDataURL(file);
      let asd = await upload_file('fakultet', `${this.data().id}`, file);
    }
  }
}
